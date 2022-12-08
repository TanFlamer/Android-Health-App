package com.example.myapp.mainActivities.login;

import android.annotation.SuppressLint;
import android.app.AlarmManager;
import android.app.AlertDialog;
import android.app.Application;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.Context;
import android.content.Intent;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;

import com.example.myapp.MainApplication;
import com.example.myapp.Notification;
import com.example.myapp.databasefiles.user.User;
import com.example.myapp.databasefiles.user.UserRepository;
import com.example.myapp.mainActivities.account.AccountActivity;

import java.util.Calendar;

public class LoginViewModel extends AndroidViewModel {

    private final UserRepository userRepository;

    //constructor for view model
    public LoginViewModel(@NonNull Application application) {
        super(application);
        MainApplication mainApplication = getApplication();
        userRepository = mainApplication.getUserRepository();
        createNotification();
    }

    //check if user exists
    public Intent validateUser(String username, String password){
        User user = userRepository.findUser(username); //check for user in database
        if(user != null && user.getPassword().equals(password)) //if user exists and password correct
            return loginUser(user); //send user to next activity
        else{ //else if user does not exist or password incorrect
            Toast.makeText(getApplication(), "Invalid Login Credentials", Toast.LENGTH_SHORT).show();
            return null; //return null and show toast
        }
    }

    //login user as guest
    public Intent loginGuest(){
        Intent intent = new Intent(getApplication(), AccountActivity.class);
        //send username as GUEST
        intent.putExtra("username", "GUEST");
        return intent;
    }

    //show alert dialog to validate login as guest
    public AlertDialog guestDialog(Context context){
        return new AlertDialog.Builder(context)
                .setTitle("Guest Login")
                .setMessage("Are you sure you want to login as guest? A shared account is used.")
                .setPositiveButton("Yes", (dialogInterface, i) -> context.startActivity(loginGuest()))
                .setNegativeButton("No", null)
                .create();
    }

    //login user as new user
    public Intent loginNewUser(){
        Intent intent = new Intent(getApplication(), AccountActivity.class);
        //send username as NEW USER
        intent.putExtra("username", "NEW USER");
        return intent;
    }

    //login user as regular user
    public Intent loginUser(User user){
        Intent intent = new Intent(getApplication(), AccountActivity.class);
        //send username as username
        intent.putExtra("username", user.getUsername());
        return intent;
    }

    //create notification to remind user to sleep at 10pm
    @SuppressLint("UnspecifiedImmutableFlag")
    public void createNotification(){
        //create notification channel
        createNotificationChannel();
        //send intent to notification class
        Intent intent = new Intent(getApplication(), Notification.class);
        //create pending intent
        PendingIntent pendingIntent = PendingIntent.getBroadcast(getApplication(), 0, intent, PendingIntent.FLAG_IMMUTABLE);
        //get alarm manager
        AlarmManager alarmManager = (AlarmManager) getApplication().getSystemService(Context.ALARM_SERVICE);
        //get calendar instance
        Calendar calendar = Calendar.getInstance();
        //set hour to 10pm
        calendar.set(Calendar.HOUR_OF_DAY, 22);
        //set minute to 0
        calendar.set(Calendar.MINUTE, 0);
        //set alarm for notification at 10pm
        alarmManager.set(AlarmManager.RTC_WAKEUP, calendar.getTimeInMillis(), pendingIntent);
    }

    private void createNotificationChannel(){
        //notification title
        String name = "Sleep Reminder Channel";
        //notification description
        String description = "Channel for Sleep Reminder";
        //notification importance
        int importance = NotificationManager.IMPORTANCE_DEFAULT;
        //create new notification channel
        NotificationChannel channel = new NotificationChannel("sleepReminder", name, importance);
        //set notification description
        channel.setDescription(description);
        //get notification manager
        NotificationManager notificationManager = getApplication().getSystemService(NotificationManager.class);
        //create notification channel
        notificationManager.createNotificationChannel(channel);
    }
}

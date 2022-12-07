package com.example.myapp;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;

import androidx.core.app.NotificationCompat;
import androidx.core.app.NotificationManagerCompat;

public class Notification extends BroadcastReceiver {
    @Override //method to run when notification intent received
    public void onReceive(Context context, Intent intent) {
        //build notification
        NotificationCompat.Builder builder = new NotificationCompat.Builder(context, "sleepReminder")
                .setSmallIcon(R.drawable.ic_sleep)
                .setContentTitle("Sleep Reminder")
                .setContentText("It is 10pm. Get ready to sleep")
                .setPriority(NotificationCompat.PRIORITY_DEFAULT);
        //get notification manager
        NotificationManagerCompat notificationManager = NotificationManagerCompat.from(context);
        //play notification
        notificationManager.notify(200, builder.build());
    }
}

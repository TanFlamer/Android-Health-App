package com.example.myapp.fragments.sleep.sleepList;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.example.myapp.R;
import com.example.myapp.databasefiles.sleep.Sleep;

import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.HashMap;
import java.util.List;

public class SleepListAdapter extends RecyclerView.Adapter<SleepListAdapter.SleepRecyclerItemViewHolder> {

    Context context;
    List<Sleep> sleepList;
    SleepListViewModel sleepListViewModel;
    HashMap<Sleep, Boolean> visibilityMap;
    HashMap<Sleep, Boolean> buttonMap;

    //constructor for sleep list adapter
    public SleepListAdapter(Context context, List<Sleep> sleepList, SleepListViewModel sleepListViewModel){
        this.context = context;
        this.sleepList = sleepList;
        this.sleepListViewModel = sleepListViewModel;
        visibilityMap = new HashMap<>();
        buttonMap = new HashMap<>();
        for(Sleep sleep : sleepList) visibilityMap.put(sleep, false);
        for(Sleep sleep : sleepList) buttonMap.put(sleep, false);
    }

    @NonNull
    @Override //get view for each sleep data
    public SleepRecyclerItemViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(context).inflate(R.layout.sleep_recycler_list_item, parent, false);
        return new SleepRecyclerItemViewHolder(view);
    }

    @SuppressLint({"SetTextI18n", "DefaultLocale"})
    @Override //bind sleep data to view holder
    public void onBindViewHolder(@NonNull SleepRecyclerItemViewHolder holder, int position) {
        //get sleep data at position
        Sleep sleep = sleepList.get(position);

        //set sleep date
        LocalDate date = Instant.ofEpochMilli(sleep.getDate()).atZone(ZoneId.systemDefault()).toLocalDate();
        holder.titleView.setText(date.toString());

        //set sleep time
        int sleepTime = sleep.getSleepTime();
        holder.sleepView.setText(String.format("%02d:%02d", sleepTime / 60, sleepTime % 60));

        //set wake time
        int wakeTime = sleep.getWakeTime();
        holder.wakeView.setText(String.format("%02d:%02d", wakeTime / 60, wakeTime % 60));

        //set sleep duration
        int duration = sleepListViewModel.getDuration(sleep);
        holder.durationView.setText(String.format("%02d:%02d", duration / 60, duration % 60));

        //change hidden layout visibility on click
        holder.layoutHidden.setVisibility(Boolean.TRUE.equals(visibilityMap.get(sleep)) ? View.VISIBLE : View.GONE);

        //change hidden button visibility on long click
        holder.buttonHidden.setVisibility(Boolean.TRUE.equals(buttonMap.get(sleep)) ? View.VISIBLE : View.GONE);
    }

    @Override //get sleep data count
    public int getItemCount() {
        return sleepList.size();
    }

    //update sleep list when sleep data list changes
    public void updateSleepList(List<Sleep> newSleepList, String data, String order){
        //clear old sleep list
        sleepList.clear();
        //add new sleep list
        sleepList.addAll(newSleepList);
        //clear old hidden layout map
        visibilityMap.clear();
        //clear old hidden button map
        buttonMap.clear();
        //sort new sleep list
        sortSleepList(data, order);
    }

    //sort sleep list
    @SuppressLint("NotifyDataSetChanged")
    public void sortSleepList(String data, String order){
        //sort sleep list
        sleepListViewModel.sortSleepList(sleepList, data, order);
        //hide all hidden layouts and buttons
        for(Sleep sleep : sleepList){
            visibilityMap.put(sleep, false);
            buttonMap.put(sleep, false);
        }
        //notify adapter dataset changed
        notifyDataSetChanged();
    }

    public class SleepRecyclerItemViewHolder extends RecyclerView.ViewHolder {

        TextView titleView, sleepView, wakeView, durationView;
        LinearLayout layoutVisible, layoutHidden, buttonHidden;
        ImageView clickEdit, clickDelete;

        public SleepRecyclerItemViewHolder(@NonNull View itemView) {
            super(itemView);
            //initialise all components
            initialiseAll();
        }

        //initialise all components
        public void initialiseAll(){
            //find all components by ID
            initialiseViewByID();
            //initialise edit button
            initialiseEditButton();
            //initialise delete button
            initialiseDeleteButton();
            //initialise on click listener
            initialiseOnClickListener();
            //initialise on long click listener
            initialiseOnLongClickListener();
        }

        //find all components by ID
        public void initialiseViewByID(){
            //find text views by ID
            initialiseTextViews();
            //find image buttons by ID
            initialiseImageButtons();
            //find layouts by ID
            initialiseLayouts();
        }

        //find text views by ID
        private void initialiseTextViews(){
            titleView = itemView.findViewById(R.id.sleepTitle);
            sleepView = itemView.findViewById(R.id.sleepTime);
            wakeView = itemView.findViewById(R.id.wakeTime);
            durationView = itemView.findViewById(R.id.sleepDuration);
        }

        //find image buttons by ID
        public void initialiseImageButtons(){
            clickEdit = itemView.findViewById(R.id.clickEdit);
            clickDelete = itemView.findViewById(R.id.clickDelete);
        }

        //find layouts by ID
        public void initialiseLayouts(){
            layoutVisible = itemView.findViewById(R.id.sleepLayoutVisible);
            layoutHidden = itemView.findViewById(R.id.sleepLayoutHidden);
            buttonHidden = itemView.findViewById(R.id.buttonHidden);
        }

        //initialise edit button
        public void initialiseEditButton(){
            clickEdit.setOnClickListener(v -> {
                //get sleep date at position
                long date = sleepList.get(getAdapterPosition()).getDate();
                //send sleep date to edit sleep data activity
                context.startActivity(sleepListViewModel.sleepEdit(date));
            });
        }

        //initialise delete button
        public void initialiseDeleteButton(){
            clickDelete.setOnClickListener(view -> {
                //get sleep data at position
                Sleep sleep = sleepList.get(getAdapterPosition());
                //call dialog to validate sleep data deletion
                sleepListViewModel.deleteDialog(context, sleep).show();
            });
        }

        //initialise on click listener
        public void initialiseOnClickListener(){
            layoutVisible.setOnClickListener(view -> {
                //get sleep data at position
                Sleep sleep = sleepList.get(getAdapterPosition());
                //invert hidden layout visibility on click
                visibilityMap.put(sleep, Boolean.FALSE.equals(visibilityMap.get(sleep)));
                //notify adapter dataset changed
                notifyItemChanged(getAdapterPosition());
            });
        }

        //initialise on long click listener
        public void initialiseOnLongClickListener(){
            layoutVisible.setOnLongClickListener(v -> {
                //get sleep data at position
                Sleep sleep = sleepList.get(getAdapterPosition());
                //invert hidden button visibility on long click
                buttonMap.put(sleep, Boolean.FALSE.equals(buttonMap.get(sleep)));
                //notify adapter dataset changed
                notifyItemChanged(getAdapterPosition());
                return true;
            });
        }
    }
}

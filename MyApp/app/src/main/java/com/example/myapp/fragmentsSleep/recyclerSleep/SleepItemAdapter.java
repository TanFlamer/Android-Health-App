package com.example.myapp.fragmentsSleep.recyclerSleep;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.example.myapp.R;

import java.util.List;

public class SleepItemAdapter extends RecyclerView.Adapter<SleepItemAdapter.SleepItemViewHolder> {

    Context context;
    List<SleepItem> sleepItemList;

    public SleepItemAdapter(Context context, List<SleepItem> sleepItemList){
        this.context = context;
        this.sleepItemList = sleepItemList;
    }

    @NonNull
    @Override
    public SleepItemViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(context).inflate(R.layout.sleep_list_item, parent, false);
        return new SleepItemViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull SleepItemViewHolder holder, int position) {
        SleepItem sleepItem = sleepItemList.get(position);

        holder.titleView.setText(sleepItem.getTitle());
        holder.dateView.setText(sleepItem.getDate());
        holder.sleepView.setText(sleepItem.getSleepTime());
        holder.wakeView.setText(sleepItem.getWakeTime());
        holder.durationView.setText(String.valueOf(sleepItem.getSleepDuration()));

        boolean isHidden = sleepItemList.get(position).isHidden();
        holder.layoutHidden.setVisibility(isHidden ? View.VISIBLE : View.GONE);
    }

    @Override
    public int getItemCount() {
        return sleepItemList.size();
    }

    public class SleepItemViewHolder extends RecyclerView.ViewHolder {

        TextView titleView, dateView, sleepView, wakeView, durationView;
        LinearLayout layoutVisible, layoutHidden;

        public SleepItemViewHolder(@NonNull View itemView) {
            super(itemView);

            titleView = itemView.findViewById(R.id.sleepTitle);
            dateView = itemView.findViewById(R.id.sleepDate);
            sleepView = itemView.findViewById(R.id.sleepTime);
            wakeView = itemView.findViewById(R.id.wakeTime);
            durationView = itemView.findViewById(R.id.sleepDuration);

            layoutVisible = itemView.findViewById(R.id.sleepLayoutVisible);
            layoutHidden = itemView.findViewById(R.id.sleepLayoutHidden);

            layoutVisible.setOnClickListener(view -> {
                SleepItem sleepItem = sleepItemList.get(getAdapterPosition());
                sleepItem.setHidden(!sleepItem.isHidden());
                notifyItemChanged(getAdapterPosition());
            });
        }
    }
}
